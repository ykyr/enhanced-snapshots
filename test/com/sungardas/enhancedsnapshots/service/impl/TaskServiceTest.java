package com.sungardas.enhancedsnapshots.service.impl;

import com.amazonaws.services.ec2.model.VolumeType;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.*;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.*;
import com.sungardas.enhancedsnapshots.dto.*;
import com.sungardas.enhancedsnapshots.service.ConfigurationService;
import com.sungardas.enhancedsnapshots.service.NotificationService;
import com.sungardas.enhancedsnapshots.service.SchedulerService;
import org.junit.*;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.*;

import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.*;


@RunWith(MockitoJUnitRunner.class)
public class TaskServiceTest {

    @InjectMocks
    private TaskServiceImpl taskService;
    private TaskDto taskDto;
    private SystemConfiguration.SystemProperties systemProperties;
    private SystemConfiguration systemConfiguration;
    private int iopsPerGb = 30;

    @Mock
    private TaskRepository taskRepository;
    @Mock
    private BackupRepository backupRepository;
    @Mock
    private SnapshotRepository snapshotRepository;
    @Mock
    private ConfigurationService configuration;
    @Mock
    private SchedulerService schedulerService;
    @Mock
    private NotificationService notificationService;


    @Before
    public void setUp(){
        ReflectionTestUtils.setField(taskService, "queueSize", 5);

        taskDto = new TaskDto();
        taskDto.setVolumes(Arrays.asList("volId-1"));
        systemConfiguration = new SystemConfiguration();
        systemProperties =  new SystemConfiguration.SystemProperties();
        systemConfiguration.setSystemProperties(systemProperties);
        systemProperties.setTempVolumeIopsPerGb(iopsPerGb);
        systemProperties.setTempVolumeType(VolumeType.Gp2.toString());
        systemProperties.setRestoreVolumeIopsPerGb(iopsPerGb);
        systemProperties.setRestoreVolumeType(VolumeType.Gp2.toString());

        when(configuration.getConfiguration()).thenReturn(new Configuration());
        when(configuration.getSystemConfiguration()).thenReturn(systemConfiguration);
        when(backupRepository.getLast(anyString(), anyString())).thenReturn(new BackupEntry());
        when(snapshotRepository.findOne((anyString()))).thenReturn(new SnapshotEntry());
    }

    @Test
    public void shouldSetIO1TempVolumeTypeForBackupTask(){
        taskDto.setType("backup");
        systemProperties.setTempVolumeIopsPerGb(iopsPerGb);
        systemProperties.setTempVolumeType(VolumeType.Io1.toString());

        taskService.createTask(taskDto);

        ArgumentCaptor<ArrayList> validTasks = ArgumentCaptor.forClass(ArrayList.class);
        verify(taskRepository).save(validTasks.capture());
        TaskEntry taskEntry = (TaskEntry) validTasks.getAllValues().get(0).get(0);

        // should set temp volume info
        Assert.assertTrue(taskEntry.getTempVolumeIopsPerGb() == iopsPerGb);
        Assert.assertTrue(taskEntry.getTempVolumeType().equals(VolumeType.Io1.toString()));
    }

    @Test
    public void shouldSetGP2TempVolumeTypeForBackupTask(){
        taskDto.setType("backup");
        systemProperties.setTempVolumeIopsPerGb(iopsPerGb);
        systemProperties.setTempVolumeType(VolumeType.Gp2.toString());

        taskService.createTask(taskDto);

        ArgumentCaptor<ArrayList> validTasks = ArgumentCaptor.forClass(ArrayList.class);
        verify(taskRepository).save(validTasks.capture());
        TaskEntry taskEntry = (TaskEntry) validTasks.getAllValues().get(0).get(0);

        // should set temp volume info
        Assert.assertTrue(taskEntry.getTempVolumeIopsPerGb() == 0);
        Assert.assertTrue(taskEntry.getTempVolumeType().equals(VolumeType.Gp2.toString()));
    }

    @Test
    public void shouldSetGP2TempVolumeTypeForRestoreTask(){
        taskDto.setType("restore");
        systemProperties.setTempVolumeIopsPerGb(iopsPerGb);
        systemProperties.setTempVolumeType(VolumeType.Gp2.toString());

        taskService.createTask(taskDto);

        ArgumentCaptor<ArrayList> validTasks = ArgumentCaptor.forClass(ArrayList.class);
        verify(taskRepository).save(validTasks.capture());
        TaskEntry taskEntry = (TaskEntry) validTasks.getAllValues().get(0).get(0);

        // should set temp volume info
        Assert.assertTrue(taskEntry.getTempVolumeIopsPerGb() == 0);
        Assert.assertTrue(taskEntry.getTempVolumeType().equals(VolumeType.Gp2.toString()));
    }


    @Test
    public void shouldSetOP1TempVolumeTypeForRestoreTask(){
        taskDto.setType("restore");
        systemProperties.setTempVolumeIopsPerGb(iopsPerGb);
        systemProperties.setTempVolumeType(VolumeType.Io1.toString());

        taskService.createTask(taskDto);

        ArgumentCaptor<ArrayList> validTasks = ArgumentCaptor.forClass(ArrayList.class);
        verify(taskRepository).save(validTasks.capture());
        TaskEntry taskEntry = (TaskEntry) validTasks.getAllValues().get(0).get(0);

        // should set temp volume info
        Assert.assertTrue(taskEntry.getTempVolumeIopsPerGb() == iopsPerGb);
        Assert.assertTrue(taskEntry.getTempVolumeType().equals(VolumeType.Io1.toString()));

    }

    @Test
    public void shouldSetOP1RestoreVolumeTypeForRestoreTask(){
        taskDto.setType("restore");
        systemProperties.setRestoreVolumeIopsPerGb(iopsPerGb);
        systemProperties.setRestoreVolumeType(VolumeType.Io1.toString());

        taskService.createTask(taskDto);

        ArgumentCaptor<ArrayList> validTasks = ArgumentCaptor.forClass(ArrayList.class);
        verify(taskRepository).save(validTasks.capture());
        TaskEntry taskEntry = (TaskEntry) validTasks.getAllValues().get(0).get(0);

        //should set restore volume info
        Assert.assertTrue(taskEntry.getRestoreVolumeIopsPerGb() == iopsPerGb);
        Assert.assertTrue(taskEntry.getRestoreVolumeType() == VolumeType.Io1.toString());
    }

    @Test
    public void shouldSetGP2RestoreVolumeTypeForRestoreTask(){
        taskDto.setType("restore");
        systemProperties.setRestoreVolumeIopsPerGb(iopsPerGb);
        systemProperties.setRestoreVolumeType(VolumeType.Gp2.toString());

        taskService.createTask(taskDto);

        ArgumentCaptor<ArrayList> validTasks = ArgumentCaptor.forClass(ArrayList.class);
        verify(taskRepository).save(validTasks.capture());
        TaskEntry taskEntry = (TaskEntry) validTasks.getAllValues().get(0).get(0);

        //should set restore volume info
        Assert.assertTrue(taskEntry.getRestoreVolumeIopsPerGb() == 0);
        Assert.assertTrue(taskEntry.getRestoreVolumeType() == VolumeType.Gp2.toString());
    }
}
