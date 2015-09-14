package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.service.CryptoService;
import org.jasypt.util.text.BasicTextEncryptor;
import org.springframework.stereotype.Service;

@Service
public class CryptoServiceImpl implements CryptoService {

    @Override
    public String encrypt(String privateKey, String value) {
        BasicTextEncryptor encryptor = new BasicTextEncryptor();
        encryptor.setPassword(privateKey);
        return encryptor.encrypt(value);
    }

    @Override
    public String decrypt(String privateKey, String encrypted) {
        BasicTextEncryptor encryptor = new BasicTextEncryptor();
        encryptor.setPassword(privateKey);
        return encryptor.decrypt(encrypted);
    }

}
