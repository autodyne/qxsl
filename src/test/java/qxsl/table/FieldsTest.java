/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table;

import org.junit.Test;
import qxsl.field.*;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Fields}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/25
 *
 */
public final class FieldsTest extends junit.framework.TestCase {
	private final Fields fields = new Fields();
	@Test
	public void testIterator() {
		assertThat(fields.iterator()).isNotNull();
		assertThat(fields.iterator()).hasNext();
	}
	@Test
	public void testGetFormat() {
		assertThat(fields.getFormat(Qxsl.BAND)).isInstanceOf(Band.Format.class);
		assertThat(fields.getFormat(Qxsl.CALL)).isInstanceOf(Call.Format.class);
		assertThat(fields.getFormat(Qxsl.CODE)).isInstanceOf(Code.Format.class);
		assertThat(fields.getFormat(Qxsl.MODE)).isInstanceOf(Mode.Format.class);
		assertThat(fields.getFormat(Qxsl.NAME)).isInstanceOf(Name.Format.class);
		assertThat(fields.getFormat(Qxsl.NOTE)).isInstanceOf(Note.Format.class);
		assertThat(fields.getFormat(Qxsl.RSTQ)).isInstanceOf(RSTQ.Format.class);
		assertThat(fields.getFormat(Qxsl.TIME)).isInstanceOf(Time.Format.class);
		assertThat(fields.getFormat(Qxsl.WATT)).isInstanceOf(Watt.Format.class);
	}
}
