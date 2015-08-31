package com.sungardas.snapdirector.aws.dynamodb.repository;

import java.util.List;

import org.socialsignin.spring.data.dynamodb.repository.EnableScan;
import org.springframework.data.repository.CrudRepository;

import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;

@EnableScan
public interface WorkerConfigurationRepository extends CrudRepository <WorkerConfiguration, String>{
}
