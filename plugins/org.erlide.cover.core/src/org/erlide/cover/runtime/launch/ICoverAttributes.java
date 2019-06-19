package org.erlide.cover.runtime.launch;

import org.erlide.core.ErlangCore;

/**
 * Names of attributes in launch configuration
 *
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 *
 */
public interface ICoverAttributes {

    String prefix = ErlangCore.PLUGIN_ID;

    String TYPE = ICoverAttributes.prefix + ".type";
    String PROJECT = ICoverAttributes.prefix + ".project";
    String MODULE = ICoverAttributes.prefix + ".module";
    String FILE = ICoverAttributes.prefix + ".file";
    String APP_PROJECT = ICoverAttributes.prefix + ".app_project";
    String APPLICATION = ICoverAttributes.prefix + ".application";

    String COMBO = ICoverAttributes.prefix + ".framework";

}
