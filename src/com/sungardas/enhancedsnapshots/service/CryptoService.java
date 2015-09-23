package com.sungardas.enhancedsnapshots.service;

public interface CryptoService {
    String encrypt(String privateKey, String value);

    String decrypt(String privateKey, String data);
}
