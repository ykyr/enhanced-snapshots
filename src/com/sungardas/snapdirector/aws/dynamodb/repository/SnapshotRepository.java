package com.sungardas.snapdirector.aws.dynamodb.repository;

import java.util.List;

import org.socialsignin.spring.data.dynamodb.repository.EnableScan;
import org.springframework.data.repository.CrudRepository;

import com.sungardas.snapdirector.aws.dynamodb.model.Snapshot;

@EnableScan
public interface SnapshotRepository extends CrudRepository<Snapshot, String> {

	List<Snapshot> findBySnapshotId(String snapshotId);
	
	List<Snapshot> findByVolumeId(String volumeId);
	
	List<Snapshot> findByVolumeIdAndSnapshotIdNot(String volumeId, String snapshotId);
	
	List<Snapshot> deleteByVolumeId(String volumeId);
	
	List<Snapshot> deleteBySnapshotId(String snapshotId);
	
	List<Snapshot> deleteBySnapshotIdNot(String snapshotId);
	
}
