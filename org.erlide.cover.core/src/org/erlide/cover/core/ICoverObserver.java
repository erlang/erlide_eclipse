package org.erlide.cover.core;

public interface ICoverObserver {
	
	public void updateViewer();
	
	public void showError(String place, String type, String info);

}
