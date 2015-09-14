package com.sungardas.snapdirector.service;

public interface CryptoService {
    String encrypt(String privateKey, String value);

    String decrypt(String privateKey, String data);
}
