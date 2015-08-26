package com.sungardas.snapdirector.service.impl;


import com.amazonaws.AmazonServiceException;
import com.amazonaws.services.ec2.AmazonEC2;
import com.amazonaws.services.ec2.model.CreateVolumeRequest;
import com.amazonaws.services.ec2.model.CreateVolumeResult;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class AWSCommunticationServiceImplTest {

	@Mock
	private AmazonEC2 ec2client;
	@InjectMocks
	private AWSCommunticationServiceImpl awsCommunticationService;
	private String snapshotId = "snap-1";
	private String availableZone = "zone-1";

	@Before
	public void setUp() {
		awsCommunticationService.setRetryRestoreAttempts(3);
		awsCommunticationService.setRetryRestoreTimeout(0);
	}

	@Test(timeout = 3000)
	public void shouldCreateVolumeFromSnapshot() {
		CreateVolumeRequest crVolumeRequest = new CreateVolumeRequest(snapshotId,
				availableZone);
		when(ec2client.createVolume(crVolumeRequest)).thenReturn(new CreateVolumeResult());
		awsCommunticationService.createVolumeFromSnapshot(snapshotId, availableZone);
		verify(ec2client, times(1)).createVolume(crVolumeRequest);
	}

	@Test(timeout = 3000)
	public void shouldRetryToCreateVolumeInCaseAmazonServiceError() {
		CreateVolumeRequest crVolumeRequest = new CreateVolumeRequest(snapshotId,
				availableZone);
		AmazonServiceException exception = new AmazonServiceException("");
		exception.setErrorType(AmazonServiceException.ErrorType.Service);

		when(ec2client.createVolume(crVolumeRequest)).thenThrow(exception);
		try {
			awsCommunticationService.createVolumeFromSnapshot(snapshotId, availableZone);
		} catch (RuntimeException e) {
			// ignore
		}
		verify(ec2client, times(awsCommunticationService.getRetryRestoreAttempts())).createVolume(crVolumeRequest);
	}

	@Test(timeout = 3000)
	public void shouldNotRetryToCreateVolumeInCaseAmazonClientError() {
		CreateVolumeRequest crVolumeRequest = new CreateVolumeRequest(snapshotId,
				availableZone);
		AmazonServiceException exception = new AmazonServiceException("");
		exception.setErrorType(AmazonServiceException.ErrorType.Client);

		when(ec2client.createVolume(crVolumeRequest)).thenThrow(exception);
		try {
			awsCommunticationService.createVolumeFromSnapshot(snapshotId, availableZone);
		} catch (RuntimeException e) {
			// ignore
		}
		verify(ec2client, times(1)).createVolume(crVolumeRequest);
	}

}
