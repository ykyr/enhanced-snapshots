package com.sungardas.enhancedsnapshots.dto;

public class ExceptionDto implements Dto{
    private String message;

    public ExceptionDto(String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}
