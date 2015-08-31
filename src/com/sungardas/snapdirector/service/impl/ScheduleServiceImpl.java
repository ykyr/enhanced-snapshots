package com.sungardas.snapdirector.service.impl;

import java.text.ParseException;
import java.util.Date;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.quartz.CronExpression;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sungardas.snapdirector.aws.dynamodb.model.Schedule;
import com.sungardas.snapdirector.aws.dynamodb.repository.ScheduleRepository;
import com.sungardas.snapdirector.dto.ScheduleDto;
import com.sungardas.snapdirector.dto.converter.ScheduleDtoConverter;
import com.sungardas.snapdirector.exception.DataAccessException;
import com.sungardas.snapdirector.service.ScheduleService;

@Service
public class ScheduleServiceImpl implements ScheduleService {

	private static final Logger LOG = LogManager.getLogger(ScheduleServiceImpl.class);
	
	@Autowired
	ScheduleRepository scheduleRepository;
	
	@Override
	public List<ScheduleDto> findSchedulersToFire() {
		
		Date nowDate = new Date();
		long now  = nowDate.getTime();
		
		return ScheduleDtoConverter.convert(scheduleRepository.findByEnabledAndNextFireLessThanEqual(true, now));
	}

	@Override
	public void setNewFireTime(ScheduleDto newSchedule) throws ParseException {
		
		if (!scheduleRepository.exists(newSchedule.getId())) {
			DataAccessException e = new DataAccessException("Schedule with such id does not exist: " + newSchedule.getId());
			LOG.info("Failed to update schedule.", e);
			throw e;
		}
		
		CronExpression exp;
		try {
			exp = new CronExpression(newSchedule.getCron());
		} catch (ParseException e) {
			LOG.info("Failed to parse cron expression.", e);
			throw e;
		}
		
		long nextFire = exp.getNextValidTimeAfter(new Date()).getTime();
		
		Schedule updateSchedule = ScheduleDtoConverter.convert(newSchedule);
		updateSchedule.setNextFire(nextFire);
		
		scheduleRepository.save(updateSchedule);
		LOG.info("New fire time set.");

	}

	@Override
	public ScheduleDto findById(String id) {
		
		return ScheduleDtoConverter.convert(scheduleRepository.findOne(id));
	}

}
