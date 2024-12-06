data "aws_availability_zones" "available" {
  state = "available"
}

resource "aws_vpc" "vpc" {
  cidr_block                       = var.cidr
  assign_generated_ipv6_cidr_block = true
  enable_dns_hostnames             = true
  enable_dns_support               = true
  tags = {
    Name = var.prefix
  }
}

data "aws_vpc" "vpc" {
  id = aws_vpc.vpc.id
}

resource "aws_subnet" "public" {
  count                           = length(data.aws_availability_zones.available.names)
  vpc_id                          = aws_vpc.vpc.id
  cidr_block                      = cidrsubnet(var.cidr, 8, count.index)
  ipv6_cidr_block                 = cidrsubnet(aws_vpc.vpc.ipv6_cidr_block, 8, count.index)
  map_public_ip_on_launch         = true
  assign_ipv6_address_on_creation = true
  availability_zone               = data.aws_availability_zones.available.names[count.index]
  tags = {
    Name = var.prefix
  }
}

resource "aws_subnet" "private" {
  count                           = length(data.aws_availability_zones.available.names)
  vpc_id                          = aws_vpc.vpc.id
  cidr_block                      = cidrsubnet(var.cidr, 8, count.index + length(data.aws_availability_zones.available.names))
  ipv6_cidr_block                 = cidrsubnet(aws_vpc.vpc.ipv6_cidr_block, 8, count.index + length(data.aws_availability_zones.available.names))
  map_public_ip_on_launch         = false
  assign_ipv6_address_on_creation = true
  availability_zone               = data.aws_availability_zones.available.names[count.index]
  tags = {
    Name = "${var.prefix}-private"
  }
}

resource "aws_internet_gateway" "igw" {
  vpc_id = aws_vpc.vpc.id
  tags = {
    Name = var.prefix
  }
}

resource "aws_route" "igw" {
  route_table_id         = aws_vpc.vpc.main_route_table_id
  gateway_id             = aws_internet_gateway.igw.id
  destination_cidr_block = "0.0.0.0/0"
}

resource "aws_route" "igw_ipv6" {
  route_table_id              = aws_vpc.vpc.main_route_table_id
  gateway_id                  = aws_internet_gateway.igw.id
  destination_ipv6_cidr_block = "::/0"
}

resource "aws_route_table_association" "public" {
  count          = length(aws_subnet.public)
  subnet_id      = aws_subnet.public[count.index].id
  route_table_id = aws_vpc.vpc.main_route_table_id
}

resource "aws_security_group" "vpc_sg" {
  name        = "${var.prefix}-vpc-sg"
  description = "VPC security group"
  vpc_id      = aws_vpc.vpc.id
}

resource "aws_iam_policy" "ec2_policy" {
  name        = "${var.prefix}-${var.vpc_region}"
  path        = "/"
  description = "Policy to provide permission to EC2"

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        "Effect" : "Allow",
        "Action" : [
          "route53:ListHostedZones",
          "route53:ListResourceRecordSets",
          "route53:ChangeResourceRecordSets",
          "route53:GetHostedZone",
          "s3:GetObject",
          "s3:List*",
          "s3:PutObject",
          "logs:CreateLogGroup",
          "logs:CreateLogStream",
          "logs:PutLogEvents"
        ],
        "Resource" : [
          "*"
        ]
      }
    ]
  })
}

resource "aws_iam_role" "ec2_role" {
  name = "${var.prefix}-${var.vpc_region}"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Sid    = ""
        Principal = {
          Service = "ec2.amazonaws.com"
        }
      },
    ]
  })
}

resource "aws_iam_policy_attachment" "ec2_policy_role" {
  name       = "${var.prefix}-${var.vpc_region}"
  roles      = [aws_iam_role.ec2_role.name]
  policy_arn = aws_iam_policy.ec2_policy.arn
}

resource "aws_iam_role_policy_attachment" "ssm_role_policy_attachment" {
  role       = aws_iam_role.ec2_role.name
  policy_arn = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
}

resource "aws_iam_instance_profile" "ec2_profile" {
  name = "${var.prefix}-${var.vpc_region}"
  role = aws_iam_role.ec2_role.name
}

module "security_group_rules" {
  source            = "./../security_group_rules"
  cidr_ipv4         = var.cidr
  security_group_id = aws_security_group.vpc_sg.id
}

resource "aws_eip" "nat" {
  domain = "vpc"
  tags = {
    Name = "${var.prefix}-nat"
  }
}

resource "aws_nat_gateway" "nat" {
  allocation_id = aws_eip.nat.id
  subnet_id     = aws_subnet.public[0].id

  tags = {
    Name = "${var.prefix}-nat"
  }

  depends_on = [aws_internet_gateway.igw]
}

resource "aws_egress_only_internet_gateway" "egress" {
  vpc_id = aws_vpc.vpc.id
}

resource "aws_route_table" "private" {
  vpc_id = aws_vpc.vpc.id

  tags = {
    Name = "${var.prefix}-private"
  }
}

resource "aws_route" "private_nat" {
  route_table_id         = aws_route_table.private.id
  nat_gateway_id         = aws_nat_gateway.nat.id
  destination_cidr_block = "0.0.0.0/0"
}

resource "aws_route" "private_ipv6_egress" {
  route_table_id              = aws_route_table.private.id
  egress_only_gateway_id      = aws_egress_only_internet_gateway.egress.id
  destination_ipv6_cidr_block = "::/0"
}

resource "aws_route_table_association" "private" {
  count          = length(aws_subnet.private)
  subnet_id      = aws_subnet.private[count.index].id
  route_table_id = aws_route_table.private.id
}

resource "aws_vpc_endpoint" "ssm" {
  vpc_id              = aws_vpc.vpc.id
  service_name        = "com.amazonaws.${var.vpc_region}.ssm"
  vpc_endpoint_type   = "Interface"
  private_dns_enabled = true
  security_group_ids  = [aws_security_group.vpc_sg.id]
  subnet_ids          = aws_subnet.private[*].id
}

resource "aws_vpc_endpoint" "ssm_messages" {
  vpc_id              = aws_vpc.vpc.id
  service_name        = "com.amazonaws.${var.vpc_region}.ssmmessages"
  vpc_endpoint_type   = "Interface"
  private_dns_enabled = true
  security_group_ids  = [aws_security_group.vpc_sg.id]
  subnet_ids          = aws_subnet.private[*].id
}

resource "aws_vpc_endpoint" "kms" {
  vpc_id              = aws_vpc.vpc.id
  service_name        = "com.amazonaws.${var.vpc_region}.kms"
  vpc_endpoint_type   = "Interface"
  private_dns_enabled = true
  security_group_ids  = [aws_security_group.vpc_sg.id]
  subnet_ids          = aws_subnet.private[*].id
}

resource "aws_vpc_endpoint" "s3" {
  vpc_id            = aws_vpc.vpc.id
  service_name      = "com.amazonaws.${var.vpc_region}.s3"
  vpc_endpoint_type = "Gateway"
  route_table_ids   = [aws_vpc.vpc.main_route_table_id]
}
