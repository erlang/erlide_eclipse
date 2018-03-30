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

    public final String TYPE = ICoverAttributes.prefix + ".type";
    public final String PROJECT = ICoverAttributes.prefix + ".project";
    public final String MODULE = ICoverAttributes.prefix + ".module";
    public final String FILE = ICoverAttributes.prefix + ".file";
    public final String APP_PROJECT = ICoverAttributes.prefix + ".app_project";
    public final String APPLICATION = ICoverAttributes.prefix + ".application";

    public final String COMBO = ICoverAttributes.prefix + ".framework";

}
