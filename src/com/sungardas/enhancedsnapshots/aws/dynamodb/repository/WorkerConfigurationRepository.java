package com.sungardas.enhancedsnapshots.aws.dynamodb.repository;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.WorkerConfiguration;
import org.socialsignin.spring.data.dynamodb.repository.EnableScan;
import org.springframework.data.repository.CrudRepository;

@EnableScan
public interface WorkerConfigurationRepository extends CrudRepository <WorkerConfiguration, String>{
}
