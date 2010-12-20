package org.erlide.eunit.runtime.launch;

import org.erlide.core.ErlangPlugin;

public interface IErlTestAttributes {

	public final String prefix = ErlangPlugin.PLUGIN_ID;
	
	public final String TYPE = prefix + ".type";
	public final String PROJECT = prefix + ".project";
	public final String MODULE = prefix + ".module";
	public final String FILE = prefix + ".file";
	public final String APP_PROJECT = prefix + ".app_project";
	public final String APPLICATION = prefix + ".application";
	
	//TODO: remove it later
	public final String COVER = prefix + ".cover";
	public final String COMBO = prefix + ".framework";

	
}
