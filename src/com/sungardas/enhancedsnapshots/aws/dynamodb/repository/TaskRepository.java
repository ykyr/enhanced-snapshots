package com.sungardas.enhancedsnapshots.aws.dynamodb.repository;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry;
import org.socialsignin.spring.data.dynamodb.repository.EnableScan;
import org.socialsignin.spring.data.dynamodb.repository.EnableScanCount;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

@EnableScan
@EnableScanCount
public interface TaskRepository extends CrudRepository<TaskEntry, String> {
    List<TaskEntry> findByStatusAndInstanceIdAndRegular(String status, String instanceId, String regular);

    List<TaskEntry> findByRegularAndVolumeAndInstanceId(String regular, String volumeId, String instanceId);

    List<TaskEntry> findByRegularAndInstanceId(String regular, String instanceId);

    List<TaskEntry> findByRegularAndInstanceIdAndEnabled(String regular, String instanceId, String enabled);

    List<TaskEntry> findByVolumeAndTypeAndInstanceIdAndOptions(String volumeId, String type, String instanceId, String options);

    List<TaskEntry> findByInstanceId(String instanceId);

    List<TaskEntry> findByExpirationDateLessThanEqualAndInstanceId(String expirationDate, String instanceId);

    List<TaskEntry> findByRegular(String regular);

    Long countByRegularAndInstanceIdAndTypeAndStatus(String regular, String instanceId, String type, String status);
}
