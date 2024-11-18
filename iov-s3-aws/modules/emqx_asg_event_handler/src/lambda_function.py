import json
import logging
import boto3
import sys
import os

logger = logging.getLogger()
logger.setLevel(logging.INFO)

autoscaling = boto3.client('autoscaling')
ec2 = boto3.client('ec2')
route53 = boto3.client('route53')

ROUTE53_ZONE_ID = os.environ['ROUTE53_ZONE_ID']
SRV_RECORD_NAME = os.environ['SRV_RECORD_NAME']
ROUTE53_TTL = os.environ.get('ROUTE53_TTL', 60)

def fetch_private_dns_name(instance_id):
    logger.info("Fetching private DNS name for instance-id: %s", instance_id)
    ec2_response = ec2.describe_instances(InstanceIds=[instance_id])
    private_dns_name = ec2_response['Reservations'][0]['Instances'][0]['PrivateDnsName']
    logger.info("Found private DNS name for instance-id %s: %s",
                instance_id, private_dns_name)

    return private_dns_name


def update_srv_record(srv_records):
    route53.change_resource_record_sets(
        HostedZoneId=ROUTE53_ZONE_ID,
        ChangeBatch={
            'Changes': [
                    {
                        'Action': 'UPSERT',
                        'ResourceRecordSet': {
                            'Name': SRV_RECORD_NAME,
                            'Type': 'SRV',
                            'TTL': ROUTE53_TTL,
                            'ResourceRecords': srv_records
                        }
                    }
            ]
        }
    )


def process_message(message):
    logger.info(f"Processing message: {message}")
    if 'LifecycleActionToken' not in message or 'LifecycleHookName' not in message or 'LifecycleTransition' not in message or 'AutoScalingGroupName' not in message or 'EC2InstanceId' not in message:
        logger.info("Ignored")
        return

    lifecycle_action_token = message['LifecycleActionToken']
    lifecycle_hook_name = message['LifecycleHookName']
    lifecycle_transition = message['LifecycleTransition']
    asg_name = message['AutoScalingGroupName']
    instance_id = message['EC2InstanceId']

    logger.info(
        f"Processing {lifecycle_transition} for instance {instance_id} in ASG {asg_name}.")

    # fetch SRV record values from SRV_RECORD_NAME in ROUTE53_ZONE_ID
    srv_records = []
    resource_record_sets_list = route53.list_resource_record_sets(
        HostedZoneId=ROUTE53_ZONE_ID, StartRecordName=SRV_RECORD_NAME, StartRecordType='SRV', MaxItems='1')
    if 'ResourceRecordSets' in resource_record_sets_list:
        resource_record_sets = resource_record_sets_list['ResourceRecordSets']
        if len(resource_record_sets) > 0:
            srv_records = resource_record_sets[0]['ResourceRecords']

    private_dns_name = fetch_private_dns_name(instance_id)
    perform_update = False

    if lifecycle_transition == "autoscaling:EC2_INSTANCE_LAUNCHING":
        srv_records.append({'Value': '10 20 5370 ' + private_dns_name})
        perform_update = True
    elif lifecycle_transition == "autoscaling:EC2_INSTANCE_TERMINATING" or lifecycle_transition == "autoscaling:EC2_INSTANCE_LAUNCH_ERROR":
        for record in srv_records:
            logger.info(record)
            if record['Value'].endswith(private_dns_name):
                srv_records.remove(record)
                perform_update = True
                break

    if perform_update:
        update_srv_record(srv_records)

    response = autoscaling.complete_lifecycle_action(
        AutoScalingGroupName=asg_name,
        LifecycleActionToken=lifecycle_action_token,
        LifecycleHookName=lifecycle_hook_name,
        LifecycleActionResult='CONTINUE'
    )
    logger.info(response)


def lambda_handler(event, context):
    logger.info(event)
    for record in event['Records']:
        message = record['Sns']['Message']
        process_message(json.loads(message))


if __name__ == "__main__":
    logging.basicConfig()
    lambda_handler(json.load(sys.stdin), None)
