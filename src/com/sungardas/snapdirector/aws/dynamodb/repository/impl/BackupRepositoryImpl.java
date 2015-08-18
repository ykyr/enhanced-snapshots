package com.sungardas.snapdirector.aws.dynamodb.repository.impl;

import java.util.ArrayList;
import java.util.Arrays;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapperConfig;
import com.sungardas.snapdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.BackupRepository;

@Repository
public class BackupRepositoryImpl implements BackupRepository {
	
	@Autowired
	AmazonDynamoDB amazonDynamoDB;
	private DynamoDBMapper mapper;
	
	@Override
	public void save(BackupEntry backup) {
		DynamoDBMapperConfig config = new DynamoDBMapperConfig(
				DynamoDBMapperConfig.SaveBehavior.CLOBBER);
		mapper.batchWrite(Arrays.asList(backup), new ArrayList<BackupEntry>(), config);
	}
	
	@PostConstruct
	private void init() {
        mapper =  new DynamoDBMapper(amazonDynamoDB);
	}

}
