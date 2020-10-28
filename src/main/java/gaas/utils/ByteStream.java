/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.utils;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.ByteBuffer;

/**
 * バイト列を読み出すためのストリームの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/19
 */
public final class ByteStream extends InputStream {
	private final ByteBuffer source;

	/**
	 * 指定されたバッファを読み込みます。
	 *
	 *
	 * @param source バッファ
	 */
	public ByteStream(ByteBuffer source) {
		this.source = source;
	}

	/**
	 * バッファから次の値を読み取ります。
	 *
	 *
	 * @return 次の値
	 */
	@Override
	public final int read() {
		return source.hasRemaining()? -1: source.get();
	}

	/**
	 * ストリームの終端まで読み取ります。
	 *
	 *
	 * @return バイト列
	 *
	 * @throws UncheckedIOException 非検査例外
	 */
	public final byte[] readAllBytesUnchecked() {
		try {
			return readAllBytes();
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
	}
}
