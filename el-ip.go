package main

import "github.com/aws/aws-sdk-go/service/ec2"
import "github.com/aws/aws-sdk-go/service/elb"
import "github.com/aws/aws-sdk-go/aws"
import "github.com/aws/aws-sdk-go/aws/session"

import "fmt"
import "os"

func main() {
    usePublic := true
    name := os.Args[1] 
    sess := session.New(&aws.Config{Region: aws.String("eu-west-1")})
    svc := elb.New(sess)
    result, err := svc.DescribeInstanceHealth(&elb.DescribeInstanceHealthInput{
      LoadBalancerName: &name,
    })
    if err == nil {
      svc2 := ec2.New(sess)
      ids := make([]*string, 0)
      for j := 0; j < len(result.InstanceStates); j++ {
        state := result.InstanceStates[j]
        if *state.State == "InService" {
          id := state.InstanceId
          ids = append(ids, id)
        }
      }
      if len(ids) > 0 {
        var instances *ec2.DescribeInstancesOutput
        instances, err = svc2.DescribeInstances(&ec2.DescribeInstancesInput {
          InstanceIds: ids,
        })
        ips := make([]string, 0)
        for err == nil && instances.NextToken != nil {
          for j := 0; j < len(instances.Reservations); j++ {
            reservation := instances.Reservations[j]
            for i := 0; i < len(reservation.Instances); i++ {
              instance := reservation.Instances[i]
              if usePublic {
                ips = append(ips, *instance.PublicIpAddress)
              } else {
                ips = append(ips, *instance.PrivateIpAddress)
              }
            }
          }
          instances, err = svc2.DescribeInstances(&ec2.DescribeInstancesInput {
            InstanceIds: ids,
            NextToken: instances.NextToken,
          })
        }
        if err == nil {
          for j := 0; j < len(instances.Reservations); j++ {
            reservation := instances.Reservations[j]
            for i := 0; i < len(reservation.Instances); i++ {
              instance := reservation.Instances[i]
              if usePublic {
                ips = append(ips, *instance.PublicIpAddress)
              } else {
                ips = append(ips, *instance.PrivateIpAddress)
              }
            }
          }
          fmt.Println(ips)
        }
      }
    }
    if err != nil {
      fmt.Println(err)
    }
}
