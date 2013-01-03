
-record(module, {name,
                 lines = [], % [{Length, String}]
                 tokens = [], % [{Length, [Token]}]
                 cachedTokens = [],
                 log = none}).
