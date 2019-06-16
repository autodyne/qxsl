/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import org.junit.Test;
import qxsl.field.*;

import static qxsl.table.secret.QxmlFields.*;
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
		assertThat(fields.getFormat(BAND)).isInstanceOf(Band.Format.class);
		assertThat(fields.getFormat(CALL)).isInstanceOf(Call.Format.class);
		assertThat(fields.getFormat(CITY)).isInstanceOf(City.Format.class);
		assertThat(fields.getFormat(CODE)).isInstanceOf(Code.Format.class);
		assertThat(fields.getFormat(MODE)).isInstanceOf(Mode.Format.class);
		assertThat(fields.getFormat(NAME)).isInstanceOf(Name.Format.class);
		assertThat(fields.getFormat(NOTE)).isInstanceOf(Note.Format.class);
		assertThat(fields.getFormat(RSTQ)).isInstanceOf(RSTQ.Format.class);
		assertThat(fields.getFormat(TIME)).isInstanceOf(Time.Format.class);
		assertThat(fields.getFormat(WATT)).isInstanceOf(Watt.Format.class);
	}
}
