package com.sungardas.enhancedsnapshots.aws.dynamodb.repository;

import java.util.List;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.RetentionEntry;

import org.socialsignin.spring.data.dynamodb.repository.DynamoDBCrudRepository;
import org.socialsignin.spring.data.dynamodb.repository.EnableScan;

@EnableScan
public interface RetentionRepository extends DynamoDBCrudRepository<RetentionEntry, String> {
    List<RetentionEntry> findByInstanceId(String instanceId);
}
