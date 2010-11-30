package org.erlide.eunit.core;

public class ObjectCoverResults extends MainCoverResults{

	private String name;
	
	public ObjectCoverResults(String name) {
		this.name = name;
	}
	
	public String getName() {
		return name;
	}
	
}
