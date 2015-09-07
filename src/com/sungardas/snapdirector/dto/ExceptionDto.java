package com.sungardas.snapdirector.dto;

/**
 * Created by Kostiantyn Glukhenko on 31.08.2015.
 */
public class ExceptionDto {
    private int status;

    private Object data;

    public ExceptionDto(final int status, final Object data) {
        this.status = status;
        this.data = data;
    }

    public int getStatus() {
        return status;
    }

    public Object getData() {
        return data;
    }
}
