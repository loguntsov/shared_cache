# Shared Cache

It is simple cache (not LRU) which uses several ETS to skip overload of ETS tables.

# Usage

Create instance of cache:

```
shared_cache:new(cache1, 5)
```

All actions see shared_cache module or tests.

Thank you.

# License

MIT
