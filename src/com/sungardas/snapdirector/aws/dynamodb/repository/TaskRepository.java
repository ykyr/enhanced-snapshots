package com.sungardas.snapdirector.aws.dynamodb.repository;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import org.socialsignin.spring.data.dynamodb.repository.EnableScan;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

@EnableScan
public interface TaskRepository extends CrudRepository<TaskEntry, String> {
    List<TaskEntry> findByStatusAndInstanceIdAndRegular(String status, String instanceId, String regular);

    List<TaskEntry> findByInstanceIdAndRegular(String instanceId, String regular);

    List<TaskEntry> findByRegularAndVolume(String regular, String volumeId);

    List<TaskEntry> findByRegular(String regular);
}
