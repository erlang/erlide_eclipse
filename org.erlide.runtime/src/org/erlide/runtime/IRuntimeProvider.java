package org.erlide.runtime;

public interface IRuntimeProvider {

    IRpcSite get();

    IRpcSite get(RuntimeVersion version);

    IRpcSite get(String name);

}
