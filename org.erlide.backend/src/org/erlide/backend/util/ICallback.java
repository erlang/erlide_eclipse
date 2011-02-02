/*
 * Created on 13/10/2005
 */
package org.erlide.backend.util;

public interface ICallback<Ret, Arg> {

    Ret call(Arg arg);
}
