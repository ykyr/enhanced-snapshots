package com.sungardas.snapdirector.aws.dynamodb.repository;

import com.sungardas.snapdirector.aws.dynamodb.model.RetentionEntry;
import org.socialsignin.spring.data.dynamodb.repository.DynamoDBCrudRepository;
import org.socialsignin.spring.data.dynamodb.repository.EnableScan;

import java.util.List;

@EnableScan
public interface RetentionRepository extends DynamoDBCrudRepository<RetentionEntry, String> {
    List<RetentionEntry> findByVolumeIdAndInstanceId(String volumeId, String instanceId);

    List<RetentionEntry> findByInstanceId(String instanceId);

    void deleteByVolumeIdAndInstanceId(String volumeId, String instanceId);

}
