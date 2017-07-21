package org.erlide.ui.properties;

import java.util.Collection;

import org.eclipse.core.databinding.conversion.IConverter;
import org.erlide.engine.model.root.PathSerializer;

public class StringToIPathListConverter implements IConverter {
    @Override
    public Object getToType() {
        return Collection.class;
    }

    @Override
    public Object getFromType() {
        return String.class;
    }

    @Override
    public Object convert(final Object fromObject) {
        return PathSerializer.unpackList((String) fromObject);
    }
}
