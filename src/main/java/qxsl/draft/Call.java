/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.draft;

import qxsl.utils.AssetUtil;

import static java.text.Normalizer.Form.NFKC;
import static java.text.Normalizer.normalize;

/**
 * 相手の呼出符号を表す属性の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Call extends Qxsl<String> {
	/**
	 * 呼出符号を指定して属性を構築します。
	 *
	 *
	 * @param call 呼出符号
	 */
	public Call(String call) {
		super(CALL, normalize(call.toUpperCase(), NFKC));
	}

	/**
	 * 斜線より前の呼出符号を返します。
	 *
	 *
	 * @return 呼出符号
	 */
	public final String strip() {
		return value().split("/", 2)[0];
	}

	/**
	 * この属性の値が有効か検証します。
	 *
	 *
	 * @return 有効な場合は真
	 *
	 * @since 2022/08/01
	 */
	@Override
	public final boolean valid() {
		final var asset = AssetUtil.from(this);
		final var local = CALL.getLocalPart();
		final var regex = asset.string(local);
		return value().matches(regex);
	}
}
