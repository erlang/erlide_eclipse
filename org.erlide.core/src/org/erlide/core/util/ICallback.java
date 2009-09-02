/*
 * Created on 13/10/2005
 */
package org.erlide.core.util;

public interface ICallback<Ret, Arg> {

    Ret call(Arg arg);
}
