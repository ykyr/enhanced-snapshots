package com.sungardas.snapdirector.aws.dynamodb.repository.impl;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapperConfig;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBQueryExpression;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBScanExpression;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.Condition;
import com.sungardas.snapdirector.aws.dynamodb.model.BackupEntry;
import com.sungardas.snapdirector.aws.dynamodb.repository.BackupRepository;
import com.sungardas.snapdirector.exception.DataAccessException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.amazonaws.services.dynamodbv2.model.ComparisonOperator.EQ;

@Repository
public class BackupRepositoryImpl implements BackupRepository {

    @Autowired
    private AmazonDynamoDB amazonDynamoDB;

    private DynamoDBMapper mapper;

    @Override
    public void save(BackupEntry backup) {
        DynamoDBMapperConfig config = new DynamoDBMapperConfig(
                DynamoDBMapperConfig.SaveBehavior.CLOBBER);
        mapper.batchWrite(Arrays.asList(backup), new ArrayList<BackupEntry>(), config);
    }

    @Override
    public void delete(BackupEntry backupEntry) {
        List<DynamoDBMapper.FailedBatch> failed = mapper.batchDelete(backupEntry);
        if (!failed.isEmpty()) {
            throw new DataAccessException("Can`t delete: " + backupEntry.getVolumeId() + " " + backupEntry.getFileName());
        }
    }

    @Override
    public List<BackupEntry> get(String volumeId, String instanceId) {
        BackupEntry backupEntry = new BackupEntry();
        backupEntry.setVolumeId(volumeId);
        DynamoDBQueryExpression<BackupEntry> expression = new DynamoDBQueryExpression<BackupEntry>()
                .withHashKeyValues(backupEntry).withQueryFilterEntry("instanceId", new Condition()
                        .withComparisonOperator(EQ).withAttributeValueList(new AttributeValue().withS(instanceId)));

        List<BackupEntry> backupEntries = mapper.query(BackupEntry.class,
                expression);

        return backupEntries;
    }

    @Override
    public BackupEntry getLast(String volumeId, String instanceId) {
        BackupEntry backupEntry = new BackupEntry();
        backupEntry.setVolumeId(volumeId);
        DynamoDBQueryExpression<BackupEntry> expression = new DynamoDBQueryExpression<BackupEntry>()
                .withHashKeyValues(backupEntry).withScanIndexForward(false).withQueryFilterEntry("instanceId", new Condition()
                        .withComparisonOperator(EQ).withAttributeValueList(new AttributeValue().withS(instanceId)));

        List<BackupEntry> backupEntries = mapper.query(BackupEntry.class, expression);

        return getFirst(backupEntries);
    }

    @Override
    public BackupEntry getByBackupFileName(String backupName) {
        Condition condition = new Condition().
                withComparisonOperator(EQ.toString()).withAttributeValueList(new AttributeValue(backupName));
        DynamoDBScanExpression expression = new DynamoDBScanExpression().withFilterConditionEntry("fileName", condition);
        List<BackupEntry> backupEntries = mapper.scan(BackupEntry.class, expression);
        return getFirst(backupEntries);
    }

    private BackupEntry getFirst(List<BackupEntry> backupEntries) {
        if (backupEntries.isEmpty()) {
            return null;
        } else {
            return backupEntries.get(0);
        }
    }

    @PostConstruct
    private void init() {
        mapper = new DynamoDBMapper(amazonDynamoDB);
    }

    @Override
    public List<BackupEntry> findAll(String instanceId) {
        return mapper.scan(BackupEntry.class, new DynamoDBScanExpression().withFilterConditionEntry("instanceId", new Condition()
                .withComparisonOperator(EQ).withAttributeValueList(new AttributeValue().withS(instanceId))));
    }

    @Override
    public int count() {
        return mapper.count(BackupEntry.class, new DynamoDBScanExpression());

    }

}
