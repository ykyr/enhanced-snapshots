package com.sungardas.snapdirector.tasks;

import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;

@Component
@Scope("prototype")
public class RestoreFakeTask implements RestoreTask {

	@Override
	public void setTaskEntry(TaskEntry taskEntry) {
		// TODO Auto-generated method stub

	}

	@Override
	public void execute() {
		// TODO Auto-generated method stub

	}

}
