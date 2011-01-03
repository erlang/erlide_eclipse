package org.erlide.cover.core;

public interface ICoverObserver {
	
	public void finishCovering();
	
	public void showError(String place, String type, String info);

}
