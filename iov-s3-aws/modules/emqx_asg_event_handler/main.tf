locals {
  function_name = "${var.asg_name}_event_handler"
}

resource "aws_sns_topic" "asg_events_topic" {
  name = "asg-${var.asg_name}-events"
}

# Create the IAM Role for Lambda
resource "aws_iam_role" "lambda_role" {
  name = "${local.function_name}-role"
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Principal = {
          Service = "lambda.amazonaws.com"
        }
        Action = "sts:AssumeRole"
      }
    ]
  })
}

resource "aws_iam_policy" "lambda_policy" {
  name = "${local.function_name}-policy"
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect   = "Allow"
        Action   = ["logs:CreateLogStream", "logs:PutLogEvents"]
        Resource = "*"
      },
      {
        Effect = "Allow"
        Action = [
          "autoscaling:CompleteLifecycleAction",
          "ec2:DescribeInstances",
          "route53:ChangeResourceRecordSets",
          "route53:ChangeTagsForResource",
          "route53:GetChange",
          "route53:GetHostedZone",
          "route53:ListResourceRecordSets",
          "route53:ListTagsForResource",
        ]
        Resource = "*"
      }
    ]
  })
}

resource "aws_iam_role_policy_attachment" "lambda_attach" {
  role       = aws_iam_role.lambda_role.name
  policy_arn = aws_iam_policy.lambda_policy.arn
}

data "archive_file" "lambda_zip" {
  type        = "zip"
  source_dir  = "${path.module}/src"
  output_path = "${path.module}/src.zip"
}

resource "aws_lambda_function" "asg_event_handler" {
  function_name    = local.function_name
  runtime          = "python3.11"
  handler          = "lambda_function.lambda_handler"
  role             = aws_iam_role.lambda_role.arn
  filename         = data.archive_file.lambda_zip.output_path
  source_code_hash = data.archive_file.lambda_zip.output_base64sha256

  environment {
    variables = {
      ROUTE53_ZONE_ID = var.route53_zone_id
      SRV_RECORD_NAME = var.srv_record_name
    }
  }

  depends_on = [
    data.archive_file.lambda_zip,
    aws_cloudwatch_log_group.lambda_log_group
  ]
}

resource "aws_cloudwatch_log_group" "lambda_log_group" {
  name              = "/aws/lambda/${local.function_name}"
  retention_in_days = 14
}

resource "aws_sns_topic_subscription" "lambda_subscription" {
  topic_arn = aws_sns_topic.asg_events_topic.arn
  protocol  = "lambda"
  endpoint  = aws_lambda_function.asg_event_handler.arn
}

resource "aws_lambda_permission" "allow_sns" {
  statement_id  = "AllowSNSInvoke"
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.asg_event_handler.function_name
  principal     = "sns.amazonaws.com"
  source_arn    = aws_sns_topic.asg_events_topic.arn
}

resource "aws_autoscaling_lifecycle_hook" "launch_hook" {
  name                    = "launch-hook"
  autoscaling_group_name  = var.asg_name
  lifecycle_transition    = "autoscaling:EC2_INSTANCE_LAUNCHING"
  notification_target_arn = aws_sns_topic.asg_events_topic.arn
  role_arn                = aws_iam_role.sns_publish_role.arn
}

resource "aws_autoscaling_lifecycle_hook" "termination_hook" {
  name                    = "terminate-hook"
  autoscaling_group_name  = var.asg_name
  lifecycle_transition    = "autoscaling:EC2_INSTANCE_TERMINATING"
  notification_target_arn = aws_sns_topic.asg_events_topic.arn
  role_arn                = aws_iam_role.sns_publish_role.arn
}

resource "aws_iam_role" "sns_publish_role" {
  name = "${var.asg_name}-sns-publish-role"
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Principal = {
          Service = "autoscaling.amazonaws.com"
        }
        Action = "sts:AssumeRole"
      }
    ]
  })
}

resource "aws_iam_policy" "sns_publish_policy" {
  name = "${var.asg_name}-sns-publish-policy"
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect   = "Allow"
        Action   = "sns:Publish"
        Resource = aws_sns_topic.asg_events_topic.arn
      }
    ]
  })
}

resource "aws_iam_role_policy_attachment" "sns_publish_attach" {
  role       = aws_iam_role.sns_publish_role.name
  policy_arn = aws_iam_policy.sns_publish_policy.arn
}
