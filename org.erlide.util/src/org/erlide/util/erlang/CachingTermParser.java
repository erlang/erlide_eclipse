package org.erlide.util.erlang;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.base.Strings;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

public class CachingTermParser extends TermParser {

    private final LoadingCache<String, OtpErlangObject> cache;

    private CachingTermParser() {
        cache = CacheBuilder.newBuilder().expireAfterAccess(1, TimeUnit.HOURS)
                .maximumSize(250).build(new CacheLoader<String, OtpErlangObject>() {
                    @Override
                    public OtpErlangObject load(final String key)
                            throws TermParserException {
                        return doParse(key);
                    }
                });
    }

    @Override
    public OtpErlangObject parse(final String s) throws TermParserException {
        if (Strings.isNullOrEmpty(s)) {
            return null;
        }
        try {
            return cache.get(s);
        } catch (final ExecutionException e) {
            throw (TermParserException) e.getCause();
        }
    }

}
