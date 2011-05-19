package org.erlide.cover.runtime.launch;

import org.erlide.core.ErlangCore;

/**
 * Names of attributes in launch configuration
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public interface ICoverAttributes {

    public final String prefix = ErlangCore.PLUGIN_ID;

    public final String TYPE = prefix + ".type";
    public final String PROJECT = prefix + ".project";
    public final String MODULE = prefix + ".module";
    public final String FILE = prefix + ".file";
    public final String APP_PROJECT = prefix + ".app_project";
    public final String APPLICATION = prefix + ".application";

    public final String COMBO = prefix + ".framework";

}
