package clojure.lang;

import java.io.Reader;

/**
 * Created by isaac on 12/4/15.
 */
public class HereDocReader extends AFn {

    @Override
    public Object invoke(Object reader, Object charactor, Object opts, Object pendingForms) {
        Reader r = (Reader) reader;
        StringBuilder sb = new StringBuilder();

        char[] cache = null;
        int cacheLen = 0;
        for (int ch, cachedLen = 0; (ch = LispReader.read1(r)) != -1; ) {
            char c = (char) ch;
            if (cache == null) {
                sb.append(c);
                if (Character.isWhitespace(c)) {
                    cache = new char[cacheLen];
                } else {
                    cacheLen++;
                }
                continue;
            }

            if (c == sb.charAt(cachedLen)) {
                cache[cachedLen] = c;
                cachedLen++;
                if (cachedLen == cacheLen) { break; }
                continue;
            }

            sb.append(cache, 0, cachedLen);
            cachedLen = 0;
            sb.append(c);
        }

        return sb.delete(0, cacheLen).toString();
    }
}
