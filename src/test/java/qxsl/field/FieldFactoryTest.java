/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.field;

import java.util.Iterator;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * {@link FieldFactory}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/02
 */
public final class FieldFactoryTest extends Assertions {
	@ParameterizedTest
	@MethodSource("source")
	public void testTarget(FieldFactory format) {
		assertThat(format.target().getNamespaceURI()).isNotNull();
	}

	public static final Iterator<FieldFactory> source() {
		return new FieldManager().iterator();
	}
}
