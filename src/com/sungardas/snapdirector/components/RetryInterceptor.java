package com.sungardas.snapdirector.components;

import com.amazonaws.AmazonServiceException;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;

public class RetryInterceptor implements MethodInterceptor {

    @Value("${snapdirector.amazon.retry.count}")
    private int count;

    @Value("${snapdirector.amazon.retry.sleep}")
    private int sleep;

    private static final Logger LOG = LogManager.getLogger(RetryInterceptor.class);

    @Override
    public Object invoke(MethodInvocation methodInvocation) throws Throwable {
        Throwable throwable = null;
        for (int i = 0; i < count; i++) {
            try {
                LOG.info(methodInvocation);
                return methodInvocation.proceed();
            } catch (AmazonServiceException e){
                throwable = e;
            }
        }
        throw new SnapdirectorException(throwable);
    }
}
