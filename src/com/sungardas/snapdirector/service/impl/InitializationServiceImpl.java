package com.sungardas.snapdirector.service.impl;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.sqs.AmazonSQS;
import com.sungardas.snapdirector.service.InitializationService;

public class InitializationServiceImpl implements InitializationService {
    private AmazonSQS sqs;
    private AmazonDynamoDB amazonDynamoDB;

    @Override
    public boolean AWSCredentialsAreValid() {
        return true;
    }

    @Override
    public boolean isSystemInitialized() {
        boolean isOk= true;

        isOk = isOk && checkDbStructureIsValid();
        isOk = isOk && checkConfigurationExists();
        isOk = isOk && checkQueueExists();
        isOk = isOk && checkSdfs();
        isOk = isOk && checkAdminUserExists();

        return isOk;
    }

    @Override
    public boolean checkDefaultUser(String login, String passwd) {
        return true;
    }

    @Override
    public boolean checkDbStructureIsValid() {
        return true;
    }

    @Override
    public boolean checkConfigurationExists() {
        return true;
    }

    @Override
    public boolean checkQueueExists() {
        return true;
    }

    @Override
    public boolean checkSdfs() {
        return true;
    }

    @Override
    public boolean checkAdminUserExists() {
        return true;
    }



}
