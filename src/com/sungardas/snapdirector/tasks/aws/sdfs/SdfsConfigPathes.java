package com.sungardas.snapdirector.tasks.aws.sdfs;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;


public class SdfsConfigPathes {

	private Map<ConfigFiles, String> configPathMap = new HashMap<ConfigFiles, String>();

	private enum ConfigFiles {
		SDFS_POOL, DEDUP_DB_STORE, IO_LOG, CHUNK_STORE, CLUSTER, HASH_DB_STORE, VOLUME_PATH, VOLUME_KEYS;
	}


	public void setPoolConfigPath(String path) {
		if (path == null)
			throw new RuntimeException("Path is null");
		configPathMap.put(ConfigFiles.SDFS_POOL, path);
	}


	public void setDedupDbStorePath(String path) {
		if (path == null)
			throw new RuntimeException("Path is null");
		configPathMap.put(ConfigFiles.DEDUP_DB_STORE, path);
	}


	public void setIOLogPath(String path) {
		if (path == null)
			throw new RuntimeException("Path is null");
		configPathMap.put(ConfigFiles.IO_LOG, path);
	}


	public void setChunkStorePath(String path) {
		if (path == null)
			throw new RuntimeException("Path is null");
		configPathMap.put(ConfigFiles.CHUNK_STORE, path);
	}


	public void setClusterConfigPath(String path) {
		if (path == null)
			throw new RuntimeException("Path is null");
		configPathMap.put(ConfigFiles.CLUSTER, path);
	}


	public void setHashDBStorePath(String path) {
		if (path == null)
			throw new RuntimeException("Path is null");
		configPathMap.put(ConfigFiles.HASH_DB_STORE, path);
	}


	public void setVolumePath(String path) {
		if (path == null)
			throw new RuntimeException("Path is null");
		configPathMap.put(ConfigFiles.VOLUME_PATH, path);
	}


	public void setVolumeKeysPath(String path) {
		if (path == null)
			throw new RuntimeException("Path is null");
		configPathMap.put(ConfigFiles.VOLUME_KEYS, path);
	}


	public String getPoolConfigPath() {
		return configPathMap.get(ConfigFiles.SDFS_POOL);
	}


	public String getDedupDbStorePath() {
		return configPathMap.get(ConfigFiles.DEDUP_DB_STORE);
	}


	public String getIOLogPath() {
		return configPathMap.get(ConfigFiles.IO_LOG);
	}


	public String getChunkStorePath() {
		return configPathMap.get(ConfigFiles.CHUNK_STORE);
	}


	public String getClusterConfigPath() {
		return configPathMap.get(ConfigFiles.CLUSTER);
	}


	public String getHashDBStorePath() {
		return configPathMap.get(ConfigFiles.HASH_DB_STORE);
	}


	public String getVolumePath() {
		return configPathMap.get(ConfigFiles.VOLUME_PATH);
	}


	public String getVolumeKeysPath() {
		return configPathMap.get(ConfigFiles.VOLUME_KEYS);
	}


	public Collection<String> getPathes() {
		return configPathMap.values();
	}
}
