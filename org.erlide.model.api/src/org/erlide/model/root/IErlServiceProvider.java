package org.erlide.model.root;

public interface IErlServiceProvider {

    <T> T get(Class<T> serviceClass);

}
