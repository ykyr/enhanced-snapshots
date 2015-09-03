package com.sungardas.snapdirector.dto.converter;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;

import com.sungardas.snapdirector.aws.dynamodb.model.Snapshot;
import com.sungardas.snapdirector.dto.SnapshotDto;

public class SnapshotDtoConverter {

	public static Snapshot convert(SnapshotDto snapshotDto){
		Snapshot snapshot = new Snapshot();
		BeanUtils.copyProperties(snapshotDto, snapshot);
		return snapshot;
	}
	
	public static SnapshotDto convert(Snapshot snapshot){
		SnapshotDto snapshotDto = new SnapshotDto();
		BeanUtils.copyProperties(snapshot, snapshotDto);
		return snapshotDto;
	}
	
	public static List<SnapshotDto> convertToSnapshotDtoList(List<Snapshot> snapshots) {
		List<SnapshotDto> snapshotDtos = new ArrayList<SnapshotDto>();
		for (Snapshot snapshot : snapshots) {
			snapshotDtos.add(SnapshotDtoConverter.convert(snapshot));
		}
		return snapshotDtos;
	}
	
	public static List<Snapshot> convertToSnapshotList(List<SnapshotDto> snapshotDtos) {
		List<Snapshot> snapshots = new ArrayList<Snapshot>();
		for (SnapshotDto snapshotDto : snapshotDtos) {
			snapshots.add(SnapshotDtoConverter.convert(snapshotDto));
		}
		return snapshots;
	}
	
}
