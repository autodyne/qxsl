/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.junit.RandomNumberParameterExtension;

/**
 * {@link Band}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomNumberParameterExtension.class)
public final class BandTest extends Assertions {
	@Test
	public void testEquals() {
		assertThat(Band.parse("1.9MHz")).isEqualTo(new Band(1_900));
		assertThat(Band.parse("3.5MHz")).isEqualTo(new Band(3_500));
		assertThat(Band.parse("144MHz")).isEqualTo(new Band(144_000));
		assertThat(Band.parse("2.4GHz")).isEqualTo(new Band(2_400_000));
	}

	@Test
	public void testToGHzString() {
		assertThat(new Band(1_200_000).toGHzString()).isEqualTo("1.2GHz");
		assertThat(new Band(2_400_000).toGHzString()).isEqualTo("2.4GHz");
		assertThat(new Band(5_600_000).toGHzString()).isEqualTo("5.6GHz");
	}

	@Test
	public void testToKHzString() {
		assertThat(new Band(1_900).toKHzString()).isEqualTo("1900kHz");
		assertThat(new Band(3_500).toKHzString()).isEqualTo("3500kHz");
		assertThat(new Band(7_000).toKHzString()).isEqualTo("7000kHz");
	}

	@Test
	public void testToMHzString() {
		assertThat(new Band(1_900).toMHzString()).isEqualTo("1.9MHz");
		assertThat(new Band(3_500).toMHzString()).isEqualTo("3.5MHz");
		assertThat(new Band(7_000).toMHzString()).isEqualTo("7MHz");
	}

	@Test
	public void testToString() {
		assertThat(new Band(1_900)).hasToString("1.9MHz");
		assertThat(new Band(3_500)).hasToString("3.5MHz");
		assertThat(new Band(430_000)).hasToString("430MHz");
		assertThat(new Band(5_600_000)).hasToString("5.6GHz");
	}
}
