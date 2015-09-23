package com.sungardas.enhancedsnapshots.tasks;

import com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry;


public interface Task {
	void setTaskEntry(TaskEntry taskEntry);
	void execute();
}
