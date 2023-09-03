set(c_sources
	act_comm.c act_info.c act_move.c act_obj.c act_wiz.c boards.c bounty.c
	build.c clans.c color.c comm.c comments.c const.c db.c dns.c fight.c
	handler.c hashstr.c hotboot.c interp.c magic.c makeobjs.c mccp.c
	misc.c mssp.c mud_comm.c mud_prog.c player.c reset.c save.c sha256.c shops.c
	skills.c space.c special.c swskills.c tables.c track.c update.c
)
set_source_files_properties(${c_sources} PROPERTIES LANGUAGE CXX)

add_executable(swreality ${c_sources})

target_compile_options(swreality PRIVATE -std=c++23 -fpermissive)

target_link_libraries(swreality PRIVATE ZLIB::ZLIB)
