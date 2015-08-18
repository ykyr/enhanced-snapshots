package com.sungardas.snapdirector.tasks;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;


public interface Task {
	void setTaskEntry(TaskEntry taskEntry);
	void execute();
}
