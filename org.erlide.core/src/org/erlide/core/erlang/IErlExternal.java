/**
 * 
 */
package org.erlide.core.erlang;

/**
 * @author jakob
 * 
 */
public interface IErlExternal extends IErlElement, IParent, IOpenable {

    String getExternalName();

    // boolean hasModuleWithPath(String path);

    boolean isRoot();

}
