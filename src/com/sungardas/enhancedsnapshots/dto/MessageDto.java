package com.sungardas.enhancedsnapshots.dto;

import java.util.Collections;
import java.util.List;

public class MessageDto {
    private List<String> messages = Collections.emptyList();

    public MessageDto(List<String> messages) {
        this.messages = messages;
    }

    public List<String> getMessages() {
        return messages;
    }

    public void setMessages(List<String> messages) {
        this.messages = messages;
    }
}
