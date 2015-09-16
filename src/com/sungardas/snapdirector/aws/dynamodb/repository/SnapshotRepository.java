package com.sungardas.snapdirector.aws.dynamodb.repository;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.snapdirector.aws.dynamodb.model.SnapshotEntry;
import org.socialsignin.spring.data.dynamodb.repository.EnableScan;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@EnableScan
public interface SnapshotRepository extends CrudRepository<SnapshotEntry, String>{

    List<SnapshotEntry> findByInstanceId(String instanceId);

}
