package com.sungardas.enhancedsnapshots.dto;

import java.util.Collections;
import java.util.List;
import java.util.Map;

public class MessageDto {
    private Map<String, String> messages = Collections.emptyMap();

    public MessageDto(final Map<String, String> messages) {
        this.messages = messages;
    }

    public Map<String, String> getMessages() {
        return messages;
    }

    public void setMessages(final Map<String, String> messages) {
        this.messages = messages;
    }
}
