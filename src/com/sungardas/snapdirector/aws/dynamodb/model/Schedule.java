package com.sungardas.snapdirector.aws.dynamodb.model;

import java.text.ParseException;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

import org.quartz.CronExpression;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBAttribute;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTable;

@DynamoDBTable(tableName = "Schedule")
public class Schedule {

	private final Map<String, Object> attributes = new LinkedHashMap<String, Object>();
	
	public Schedule(){}
	
	public Schedule(String id, String cron, Boolean enabled, String name, String volumeId) throws ParseException {
		this.attributes.put("id", id);
		this.attributes.put("cron", cron);
		this.attributes.put("enabled", enabled);
		this.attributes.put("name", name);
		this.attributes.put("volumeId", volumeId);
		CronExpression exp = new CronExpression(cron);
		long nextFire = exp.getNextValidTimeAfter(new Date()).getTime();
		this.attributes.put("nextFire", nextFire);
	}
	
	@DynamoDBHashKey(attributeName = "id")
    public String getId() {
        return (String) attributes.get("id");
    }

    public void setId(String id) {
        attributes.put("id", id);
    }
	
    @DynamoDBAttribute(attributeName = "cron")
    public String getCron() {
        return (String) attributes.get("cron");
    }

    public void setCron(String cron) {
        attributes.put("cron", cron);
    }
    
    @DynamoDBAttribute(attributeName = "enabled")
    public Boolean getEnabled() {
        return (Boolean) attributes.get("enabled");
    }

    public void setEnabled(Boolean enabled) {
        attributes.put("enabled", enabled);
    }
    
    @DynamoDBAttribute(attributeName = "name")
    public String getName() {
        return (String) attributes.get("name");
    }

    public void setName(String name) {
        attributes.put("name", name);
    }
    
    @DynamoDBAttribute(attributeName = "nextFire")
    public Long getNextFire() {
        return (Long) attributes.get("nextFire");
    }

    public void setNextFire(Long nextFire) {
        attributes.put("nextFire", nextFire);
    }
    
    @DynamoDBAttribute(attributeName = "volumeId")
    public String getVolumeId() {
        return (String) attributes.get("volumeId");
    }

    public void setVolumeId(String volumeId) {
        attributes.put("volumeId", volumeId);
    }
	
}
