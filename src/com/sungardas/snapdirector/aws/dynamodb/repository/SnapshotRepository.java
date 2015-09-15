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

@Repository
public interface SnapshotRepository extends CrudRepository<SnapshotEntry, String>{

//    void save(SnapshotEntry snapshotEntry);
//
//    SnapshotEntry getByVolumeIdAndSnapshotId(SnapshotEntry snapshotEntry);

    List<SnapshotEntry> findByVolumeIdAndInstanceId(@Param("volumeId")String volumeId, @Param("instanceId")String instanceId);

    List<SnapshotEntry> findByInstanceId(@Param("instanceId")String instanceId);



}
