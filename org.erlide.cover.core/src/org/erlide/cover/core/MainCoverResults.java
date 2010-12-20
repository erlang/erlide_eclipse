package org.erlide.cover.core;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;


public class MainCoverResults {

	private double percent;
	private int linesTotal;
	private int linesCovered;
	private List<MainCoverResults> childRes;
	
	public MainCoverResults() {
		childRes = new LinkedList<MainCoverResults>();
	}
	
	public void setPercentage(double percent) {
		this.percent = percent;
	}
	
	public double getPercentage() {
		return percent;
	}
	
	public void setLinesTotal(int lines) {
		linesTotal = lines;
	}
	
	public int getLinesTotal() {
		return linesTotal;
	}
	
	public void setLinesCovered(int lines) {
		linesCovered = lines;
	}
	
	public int getLinesCovered() {
		return linesCovered;
	}
	
	public void addChild(MainCoverResults child) {
		childRes.add(child);
	}
	
	public Collection<MainCoverResults> getChildren() {
		return childRes;
	}
	
}
