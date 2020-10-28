/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.junit.RandomNumberParameterExtension;

/**
 * {@link RSTQ}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomNumberParameterExtension.class)
public final class RSTQTest extends Assertions {
	@Test
	public void testValue() {
		assertThat(new RSTQ(699).value()).isEqualTo(699);
		assertThat(new RSTQ(599).value()).isEqualTo(599);
		assertThat(new RSTQ(590).value()).isEqualTo(590);
		assertThat(new RSTQ(101).value()).isEqualTo(101);
		assertThat(new RSTQ(100).value()).isEqualTo(100);
		assertThat(new RSTQ(59).value()).isEqualTo(59);
		assertThat(new RSTQ(11).value()).isEqualTo(11);
		assertThat(new RSTQ(10).value()).isEqualTo(10);
	}

	@Test
	public void testToString() {
		assertThat(new RSTQ(699)).hasToString("699");
		assertThat(new RSTQ(599)).hasToString("599");
		assertThat(new RSTQ(590)).hasToString("590");
		assertThat(new RSTQ(101)).hasToString("101");
		assertThat(new RSTQ(100)).hasToString("100");
		assertThat(new RSTQ(59)).hasToString("59");
		assertThat(new RSTQ(11)).hasToString("11");
		assertThat(new RSTQ(10)).hasToString("10");
	}
}
