package com.sungardas.snapdirector.aws.dynamodb.repository;

import com.sungardas.snapdirector.aws.dynamodb.model.RetentionEntry;
import org.socialsignin.spring.data.dynamodb.repository.DynamoDBCrudRepository;
import org.socialsignin.spring.data.dynamodb.repository.EnableScan;

@EnableScan
public interface RetentionRepository extends DynamoDBCrudRepository<RetentionEntry, String> {
}
