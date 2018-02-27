# Shared Cache

It is simple cache (not LRU) which uses several ETS to skip overload of ETS tables.

# Usage

Init instance of cache:

```
shared_cache:init(cache1, 5)
```

All actions see shared_cache module

Thank you.

# License

MIT
