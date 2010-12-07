package org.erlide.cover.runtime.launch;

import org.erlide.core.ErlangPlugin;

public class ICoverAttributes {

public final String prefix = ErlangPlugin.PLUGIN_ID;
    
    //TODO: change it, that it could several resting frameworks

    public final String TYPE = prefix + ".type";
    public final String PROJECT = prefix + ".project";
    public final String MODULE = prefix + ".module";
    public final String FILE = prefix + ".file";
    public final String APP_PROJECT = prefix + ".app_project";
    public final String APPLICATION = prefix + ".application";
    
    public final String COVER = prefix + ".cover";
    
}
