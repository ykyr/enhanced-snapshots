package com.sungardas.snapdirector.tasks;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope("prototype")
public class DeleteFakeTask implements DeleteTask {


    @Override
    public void setTaskEntry(TaskEntry taskEntry) {

    }

    @Override
    public void execute() {

    }
}
