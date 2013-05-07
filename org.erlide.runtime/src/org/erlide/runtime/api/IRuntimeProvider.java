package org.erlide.runtime.api;


public interface IRuntimeProvider {

    IRpcSite get();

    IRpcSite get(RuntimeVersion version);

    IRpcSite get(String project);

}
