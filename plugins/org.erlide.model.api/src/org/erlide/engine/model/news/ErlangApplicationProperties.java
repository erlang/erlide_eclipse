package org.erlide.engine.model.news;

import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

/**
 * From *.app
 */
@Data
@SuppressWarnings("all")
public class ErlangApplicationProperties {
    private final String version;

    public ErlangApplicationProperties(final String version) {
        super();
        this.version = version;
    }

    @Override
    @Pure
    public int hashCode() {
        return 31 * 1 + (version == null ? 0 : version.hashCode());
    }

    @Override
    @Pure
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final ErlangApplicationProperties other = (ErlangApplicationProperties) obj;
        if (version == null) {
            if (other.version != null) {
                return false;
            }
        } else if (!version.equals(other.version)) {
            return false;
        }
        return true;
    }

    @Override
    @Pure
    public String toString() {
        final ToStringBuilder b = new ToStringBuilder(this);
        b.add("version", version);
        return b.toString();
    }

    @Pure
    public String getVersion() {
        return version;
    }
}
