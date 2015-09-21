package com.sungardas.snapdirector.aws.dynamodb.repository;

import java.util.List;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;

import org.socialsignin.spring.data.dynamodb.repository.EnableScan;
import org.springframework.data.repository.CrudRepository;

@EnableScan
public interface TaskRepository extends CrudRepository<TaskEntry, String> {
    List<TaskEntry> findByStatusAndInstanceIdAndRegular(String status, String instanceId, String regular);

    List<TaskEntry> findByRegularAndVolumeAndInstanceId(String regular, String volumeId, String instanceId);

    List<TaskEntry> findByRegularAndInstanceId(String regular, String instanceId);

    List<TaskEntry> findByRegularAndInstanceIdAndEnabled(String regular, String instanceId, String enabled);

    List<TaskEntry> findByVolumeAndTypeAndInstanceIdAndOptions(String volumeId, String type, String instanceId, String options);

    List<TaskEntry> findByInstanceId(String instanceId);
}
